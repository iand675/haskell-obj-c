{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKPaymentAuthorizationController@.
module ObjC.PassKit.PKPaymentAuthorizationController
  ( PKPaymentAuthorizationController
  , IsPKPaymentAuthorizationController(..)
  , canMakePayments
  , canMakePaymentsUsingNetworks
  , canMakePaymentsUsingNetworks_capabilities
  , initWithPaymentRequest
  , presentWithCompletion
  , dismissWithCompletion
  , supportsDisbursements
  , supportsDisbursementsUsingNetworks
  , supportsDisbursementsUsingNetworks_capabilities
  , initWithDisbursementRequest
  , delegate
  , setDelegate
  , canMakePaymentsSelector
  , canMakePaymentsUsingNetworksSelector
  , canMakePaymentsUsingNetworks_capabilitiesSelector
  , delegateSelector
  , dismissWithCompletionSelector
  , initWithDisbursementRequestSelector
  , initWithPaymentRequestSelector
  , presentWithCompletionSelector
  , setDelegateSelector
  , supportsDisbursementsSelector
  , supportsDisbursementsUsingNetworksSelector
  , supportsDisbursementsUsingNetworks_capabilitiesSelector

  -- * Enum types
  , PKMerchantCapability(PKMerchantCapability)
  , pattern PKMerchantCapability3DS
  , pattern PKMerchantCapabilityEMV
  , pattern PKMerchantCapabilityCredit
  , pattern PKMerchantCapabilityDebit
  , pattern PKMerchantCapabilityInstantFundsOut

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

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' canMakePaymentsSelector

-- | @+ canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
canMakePaymentsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' canMakePaymentsUsingNetworksSelector (toNSArray supportedNetworks)

-- | @+ canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
canMakePaymentsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' canMakePaymentsUsingNetworks_capabilitiesSelector (toNSArray supportedNetworks) capabilties

-- | @- initWithPaymentRequest:@
initWithPaymentRequest :: (IsPKPaymentAuthorizationController pkPaymentAuthorizationController, IsPKPaymentRequest request) => pkPaymentAuthorizationController -> request -> IO (Id PKPaymentAuthorizationController)
initWithPaymentRequest pkPaymentAuthorizationController request =
  sendOwnedMessage pkPaymentAuthorizationController initWithPaymentRequestSelector (toPKPaymentRequest request)

-- | @- presentWithCompletion:@
presentWithCompletion :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> Ptr () -> IO ()
presentWithCompletion pkPaymentAuthorizationController completion =
  sendMessage pkPaymentAuthorizationController presentWithCompletionSelector completion

-- | @- dismissWithCompletion:@
dismissWithCompletion :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> Ptr () -> IO ()
dismissWithCompletion pkPaymentAuthorizationController completion =
  sendMessage pkPaymentAuthorizationController dismissWithCompletionSelector completion

-- | @+ supportsDisbursements@
supportsDisbursements :: IO Bool
supportsDisbursements  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' supportsDisbursementsSelector

-- | @+ supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
supportsDisbursementsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' supportsDisbursementsUsingNetworksSelector (toNSArray supportedNetworks)

-- | @+ supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
supportsDisbursementsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    sendClassMessage cls' supportsDisbursementsUsingNetworks_capabilitiesSelector (toNSArray supportedNetworks) capabilties

-- | @- initWithDisbursementRequest:@
initWithDisbursementRequest :: (IsPKPaymentAuthorizationController pkPaymentAuthorizationController, IsPKDisbursementRequest request) => pkPaymentAuthorizationController -> request -> IO (Id PKPaymentAuthorizationController)
initWithDisbursementRequest pkPaymentAuthorizationController request =
  sendOwnedMessage pkPaymentAuthorizationController initWithDisbursementRequestSelector (toPKDisbursementRequest request)

-- | @- delegate@
delegate :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> IO RawId
delegate pkPaymentAuthorizationController =
  sendMessage pkPaymentAuthorizationController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> RawId -> IO ()
setDelegate pkPaymentAuthorizationController value =
  sendMessage pkPaymentAuthorizationController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector '[] Bool
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworksSelector :: Selector '[Id NSArray] Bool
canMakePaymentsUsingNetworksSelector = mkSelector "canMakePaymentsUsingNetworks:"

-- | @Selector@ for @canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilitiesSelector :: Selector '[Id NSArray, PKMerchantCapability] Bool
canMakePaymentsUsingNetworks_capabilitiesSelector = mkSelector "canMakePaymentsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithPaymentRequest:@
initWithPaymentRequestSelector :: Selector '[Id PKPaymentRequest] (Id PKPaymentAuthorizationController)
initWithPaymentRequestSelector = mkSelector "initWithPaymentRequest:"

-- | @Selector@ for @presentWithCompletion:@
presentWithCompletionSelector :: Selector '[Ptr ()] ()
presentWithCompletionSelector = mkSelector "presentWithCompletion:"

-- | @Selector@ for @dismissWithCompletion:@
dismissWithCompletionSelector :: Selector '[Ptr ()] ()
dismissWithCompletionSelector = mkSelector "dismissWithCompletion:"

-- | @Selector@ for @supportsDisbursements@
supportsDisbursementsSelector :: Selector '[] Bool
supportsDisbursementsSelector = mkSelector "supportsDisbursements"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworksSelector :: Selector '[Id NSArray] Bool
supportsDisbursementsUsingNetworksSelector = mkSelector "supportsDisbursementsUsingNetworks:"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilitiesSelector :: Selector '[Id NSArray, PKMerchantCapability] Bool
supportsDisbursementsUsingNetworks_capabilitiesSelector = mkSelector "supportsDisbursementsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithDisbursementRequest:@
initWithDisbursementRequestSelector :: Selector '[Id PKDisbursementRequest] (Id PKPaymentAuthorizationController)
initWithDisbursementRequestSelector = mkSelector "initWithDisbursementRequest:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

