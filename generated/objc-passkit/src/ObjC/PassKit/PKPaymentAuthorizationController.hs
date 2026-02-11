{-# LANGUAGE PatternSynonyms #-}
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
  , canMakePaymentsSelector
  , canMakePaymentsUsingNetworksSelector
  , canMakePaymentsUsingNetworks_capabilitiesSelector
  , initWithPaymentRequestSelector
  , presentWithCompletionSelector
  , dismissWithCompletionSelector
  , supportsDisbursementsSelector
  , supportsDisbursementsUsingNetworksSelector
  , supportsDisbursementsUsingNetworks_capabilitiesSelector
  , initWithDisbursementRequestSelector

  -- * Enum types
  , PKMerchantCapability(PKMerchantCapability)
  , pattern PKMerchantCapability3DS
  , pattern PKMerchantCapabilityEMV
  , pattern PKMerchantCapabilityCredit
  , pattern PKMerchantCapabilityDebit
  , pattern PKMerchantCapabilityInstantFundsOut

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

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePayments") retCULong []

-- | @+ canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
canMakePaymentsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePaymentsUsingNetworks:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ())]

-- | @+ canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
canMakePaymentsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePaymentsUsingNetworks:capabilities:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ()), argCULong (coerce capabilties)]

-- | @- initWithPaymentRequest:@
initWithPaymentRequest :: (IsPKPaymentAuthorizationController pkPaymentAuthorizationController, IsPKPaymentRequest request) => pkPaymentAuthorizationController -> request -> IO (Id PKPaymentAuthorizationController)
initWithPaymentRequest pkPaymentAuthorizationController  request =
withObjCPtr request $ \raw_request ->
    sendMsg pkPaymentAuthorizationController (mkSelector "initWithPaymentRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- | @- presentWithCompletion:@
presentWithCompletion :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> Ptr () -> IO ()
presentWithCompletion pkPaymentAuthorizationController  completion =
  sendMsg pkPaymentAuthorizationController (mkSelector "presentWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- dismissWithCompletion:@
dismissWithCompletion :: IsPKPaymentAuthorizationController pkPaymentAuthorizationController => pkPaymentAuthorizationController -> Ptr () -> IO ()
dismissWithCompletion pkPaymentAuthorizationController  completion =
  sendMsg pkPaymentAuthorizationController (mkSelector "dismissWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @+ supportsDisbursements@
supportsDisbursements :: IO Bool
supportsDisbursements  =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursements") retCULong []

-- | @+ supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworks :: IsNSArray supportedNetworks => supportedNetworks -> IO Bool
supportsDisbursementsUsingNetworks supportedNetworks =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursementsUsingNetworks:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ())]

-- | @+ supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilities :: IsNSArray supportedNetworks => supportedNetworks -> PKMerchantCapability -> IO Bool
supportsDisbursementsUsingNetworks_capabilities supportedNetworks capabilties =
  do
    cls' <- getRequiredClass "PKPaymentAuthorizationController"
    withObjCPtr supportedNetworks $ \raw_supportedNetworks ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsDisbursementsUsingNetworks:capabilities:") retCULong [argPtr (castPtr raw_supportedNetworks :: Ptr ()), argCULong (coerce capabilties)]

-- | @- initWithDisbursementRequest:@
initWithDisbursementRequest :: (IsPKPaymentAuthorizationController pkPaymentAuthorizationController, IsPKDisbursementRequest request) => pkPaymentAuthorizationController -> request -> IO (Id PKPaymentAuthorizationController)
initWithDisbursementRequest pkPaymentAuthorizationController  request =
withObjCPtr request $ \raw_request ->
    sendMsg pkPaymentAuthorizationController (mkSelector "initWithDisbursementRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @canMakePaymentsUsingNetworks:@
canMakePaymentsUsingNetworksSelector :: Selector
canMakePaymentsUsingNetworksSelector = mkSelector "canMakePaymentsUsingNetworks:"

-- | @Selector@ for @canMakePaymentsUsingNetworks:capabilities:@
canMakePaymentsUsingNetworks_capabilitiesSelector :: Selector
canMakePaymentsUsingNetworks_capabilitiesSelector = mkSelector "canMakePaymentsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithPaymentRequest:@
initWithPaymentRequestSelector :: Selector
initWithPaymentRequestSelector = mkSelector "initWithPaymentRequest:"

-- | @Selector@ for @presentWithCompletion:@
presentWithCompletionSelector :: Selector
presentWithCompletionSelector = mkSelector "presentWithCompletion:"

-- | @Selector@ for @dismissWithCompletion:@
dismissWithCompletionSelector :: Selector
dismissWithCompletionSelector = mkSelector "dismissWithCompletion:"

-- | @Selector@ for @supportsDisbursements@
supportsDisbursementsSelector :: Selector
supportsDisbursementsSelector = mkSelector "supportsDisbursements"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:@
supportsDisbursementsUsingNetworksSelector :: Selector
supportsDisbursementsUsingNetworksSelector = mkSelector "supportsDisbursementsUsingNetworks:"

-- | @Selector@ for @supportsDisbursementsUsingNetworks:capabilities:@
supportsDisbursementsUsingNetworks_capabilitiesSelector :: Selector
supportsDisbursementsUsingNetworks_capabilitiesSelector = mkSelector "supportsDisbursementsUsingNetworks:capabilities:"

-- | @Selector@ for @initWithDisbursementRequest:@
initWithDisbursementRequestSelector :: Selector
initWithDisbursementRequestSelector = mkSelector "initWithDisbursementRequest:"

