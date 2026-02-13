{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VSUserAccount@.
module ObjC.VideoSubscriberAccount.VSUserAccount
  ( VSUserAccount
  , IsVSUserAccount(..)
  , init_
  , new
  , initWithAccountType_updateURL
  , updateURL
  , setUpdateURL
  , requiresSystemTrust
  , setRequiresSystemTrust
  , accountProviderIdentifier
  , setAccountProviderIdentifier
  , identifier
  , setIdentifier
  , accountType
  , setAccountType
  , signedOut
  , setSignedOut
  , subscriptionBillingCycleEndDate
  , setSubscriptionBillingCycleEndDate
  , tierIdentifiers
  , setTierIdentifiers
  , billingIdentifier
  , setBillingIdentifier
  , authenticationData
  , setAuthenticationData
  , fromCurrentDevice
  , deviceCategory
  , appleSubscription
  , setAppleSubscription
  , accountProviderIdentifierSelector
  , accountTypeSelector
  , appleSubscriptionSelector
  , authenticationDataSelector
  , billingIdentifierSelector
  , deviceCategorySelector
  , fromCurrentDeviceSelector
  , identifierSelector
  , initSelector
  , initWithAccountType_updateURLSelector
  , newSelector
  , requiresSystemTrustSelector
  , setAccountProviderIdentifierSelector
  , setAccountTypeSelector
  , setAppleSubscriptionSelector
  , setAuthenticationDataSelector
  , setBillingIdentifierSelector
  , setIdentifierSelector
  , setRequiresSystemTrustSelector
  , setSignedOutSelector
  , setSubscriptionBillingCycleEndDateSelector
  , setTierIdentifiersSelector
  , setUpdateURLSelector
  , signedOutSelector
  , subscriptionBillingCycleEndDateSelector
  , tierIdentifiersSelector
  , updateURLSelector

  -- * Enum types
  , VSOriginatingDeviceCategory(VSOriginatingDeviceCategory)
  , pattern VSOriginatingDeviceCategoryMobile
  , pattern VSOriginatingDeviceCategoryOther
  , VSUserAccountType(VSUserAccountType)
  , pattern VSUserAccountTypeFree
  , pattern VSUserAccountTypePaid

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.VideoSubscriberAccount.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id VSUserAccount)
init_ vsUserAccount =
  sendOwnedMessage vsUserAccount initSelector

-- | @+ new@
new :: IO (Id VSUserAccount)
new  =
  do
    cls' <- getRequiredClass "VSUserAccount"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithAccountType:updateURL:@
initWithAccountType_updateURL :: (IsVSUserAccount vsUserAccount, IsNSURL url) => vsUserAccount -> VSUserAccountType -> url -> IO (Id VSUserAccount)
initWithAccountType_updateURL vsUserAccount accountType url =
  sendOwnedMessage vsUserAccount initWithAccountType_updateURLSelector accountType (toNSURL url)

-- | @- updateURL@
updateURL :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSURL)
updateURL vsUserAccount =
  sendMessage vsUserAccount updateURLSelector

-- | @- setUpdateURL:@
setUpdateURL :: (IsVSUserAccount vsUserAccount, IsNSURL value) => vsUserAccount -> value -> IO ()
setUpdateURL vsUserAccount value =
  sendMessage vsUserAccount setUpdateURLSelector (toNSURL value)

-- | @- requiresSystemTrust@
requiresSystemTrust :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
requiresSystemTrust vsUserAccount =
  sendMessage vsUserAccount requiresSystemTrustSelector

-- | @- setRequiresSystemTrust:@
setRequiresSystemTrust :: IsVSUserAccount vsUserAccount => vsUserAccount -> Bool -> IO ()
setRequiresSystemTrust vsUserAccount value =
  sendMessage vsUserAccount setRequiresSystemTrustSelector value

-- | @- accountProviderIdentifier@
accountProviderIdentifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
accountProviderIdentifier vsUserAccount =
  sendMessage vsUserAccount accountProviderIdentifierSelector

-- | @- setAccountProviderIdentifier:@
setAccountProviderIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setAccountProviderIdentifier vsUserAccount value =
  sendMessage vsUserAccount setAccountProviderIdentifierSelector (toNSString value)

-- | @- identifier@
identifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
identifier vsUserAccount =
  sendMessage vsUserAccount identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setIdentifier vsUserAccount value =
  sendMessage vsUserAccount setIdentifierSelector (toNSString value)

-- | @- accountType@
accountType :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO VSUserAccountType
accountType vsUserAccount =
  sendMessage vsUserAccount accountTypeSelector

-- | @- setAccountType:@
setAccountType :: IsVSUserAccount vsUserAccount => vsUserAccount -> VSUserAccountType -> IO ()
setAccountType vsUserAccount value =
  sendMessage vsUserAccount setAccountTypeSelector value

-- | @- signedOut@
signedOut :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
signedOut vsUserAccount =
  sendMessage vsUserAccount signedOutSelector

-- | @- setSignedOut:@
setSignedOut :: IsVSUserAccount vsUserAccount => vsUserAccount -> Bool -> IO ()
setSignedOut vsUserAccount value =
  sendMessage vsUserAccount setSignedOutSelector value

-- | @- subscriptionBillingCycleEndDate@
subscriptionBillingCycleEndDate :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSDate)
subscriptionBillingCycleEndDate vsUserAccount =
  sendMessage vsUserAccount subscriptionBillingCycleEndDateSelector

-- | @- setSubscriptionBillingCycleEndDate:@
setSubscriptionBillingCycleEndDate :: (IsVSUserAccount vsUserAccount, IsNSDate value) => vsUserAccount -> value -> IO ()
setSubscriptionBillingCycleEndDate vsUserAccount value =
  sendMessage vsUserAccount setSubscriptionBillingCycleEndDateSelector (toNSDate value)

-- | @- tierIdentifiers@
tierIdentifiers :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSArray)
tierIdentifiers vsUserAccount =
  sendMessage vsUserAccount tierIdentifiersSelector

-- | @- setTierIdentifiers:@
setTierIdentifiers :: (IsVSUserAccount vsUserAccount, IsNSArray value) => vsUserAccount -> value -> IO ()
setTierIdentifiers vsUserAccount value =
  sendMessage vsUserAccount setTierIdentifiersSelector (toNSArray value)

-- | @- billingIdentifier@
billingIdentifier :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
billingIdentifier vsUserAccount =
  sendMessage vsUserAccount billingIdentifierSelector

-- | @- setBillingIdentifier:@
setBillingIdentifier :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setBillingIdentifier vsUserAccount value =
  sendMessage vsUserAccount setBillingIdentifierSelector (toNSString value)

-- | @- authenticationData@
authenticationData :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id NSString)
authenticationData vsUserAccount =
  sendMessage vsUserAccount authenticationDataSelector

-- | @- setAuthenticationData:@
setAuthenticationData :: (IsVSUserAccount vsUserAccount, IsNSString value) => vsUserAccount -> value -> IO ()
setAuthenticationData vsUserAccount value =
  sendMessage vsUserAccount setAuthenticationDataSelector (toNSString value)

-- | @- fromCurrentDevice@
fromCurrentDevice :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO Bool
fromCurrentDevice vsUserAccount =
  sendMessage vsUserAccount fromCurrentDeviceSelector

-- | @- deviceCategory@
deviceCategory :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO VSOriginatingDeviceCategory
deviceCategory vsUserAccount =
  sendMessage vsUserAccount deviceCategorySelector

-- | @- appleSubscription@
appleSubscription :: IsVSUserAccount vsUserAccount => vsUserAccount -> IO (Id VSAppleSubscription)
appleSubscription vsUserAccount =
  sendMessage vsUserAccount appleSubscriptionSelector

-- | @- setAppleSubscription:@
setAppleSubscription :: (IsVSUserAccount vsUserAccount, IsVSAppleSubscription value) => vsUserAccount -> value -> IO ()
setAppleSubscription vsUserAccount value =
  sendMessage vsUserAccount setAppleSubscriptionSelector (toVSAppleSubscription value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VSUserAccount)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VSUserAccount)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAccountType:updateURL:@
initWithAccountType_updateURLSelector :: Selector '[VSUserAccountType, Id NSURL] (Id VSUserAccount)
initWithAccountType_updateURLSelector = mkSelector "initWithAccountType:updateURL:"

-- | @Selector@ for @updateURL@
updateURLSelector :: Selector '[] (Id NSURL)
updateURLSelector = mkSelector "updateURL"

-- | @Selector@ for @setUpdateURL:@
setUpdateURLSelector :: Selector '[Id NSURL] ()
setUpdateURLSelector = mkSelector "setUpdateURL:"

-- | @Selector@ for @requiresSystemTrust@
requiresSystemTrustSelector :: Selector '[] Bool
requiresSystemTrustSelector = mkSelector "requiresSystemTrust"

-- | @Selector@ for @setRequiresSystemTrust:@
setRequiresSystemTrustSelector :: Selector '[Bool] ()
setRequiresSystemTrustSelector = mkSelector "setRequiresSystemTrust:"

-- | @Selector@ for @accountProviderIdentifier@
accountProviderIdentifierSelector :: Selector '[] (Id NSString)
accountProviderIdentifierSelector = mkSelector "accountProviderIdentifier"

-- | @Selector@ for @setAccountProviderIdentifier:@
setAccountProviderIdentifierSelector :: Selector '[Id NSString] ()
setAccountProviderIdentifierSelector = mkSelector "setAccountProviderIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @accountType@
accountTypeSelector :: Selector '[] VSUserAccountType
accountTypeSelector = mkSelector "accountType"

-- | @Selector@ for @setAccountType:@
setAccountTypeSelector :: Selector '[VSUserAccountType] ()
setAccountTypeSelector = mkSelector "setAccountType:"

-- | @Selector@ for @signedOut@
signedOutSelector :: Selector '[] Bool
signedOutSelector = mkSelector "signedOut"

-- | @Selector@ for @setSignedOut:@
setSignedOutSelector :: Selector '[Bool] ()
setSignedOutSelector = mkSelector "setSignedOut:"

-- | @Selector@ for @subscriptionBillingCycleEndDate@
subscriptionBillingCycleEndDateSelector :: Selector '[] (Id NSDate)
subscriptionBillingCycleEndDateSelector = mkSelector "subscriptionBillingCycleEndDate"

-- | @Selector@ for @setSubscriptionBillingCycleEndDate:@
setSubscriptionBillingCycleEndDateSelector :: Selector '[Id NSDate] ()
setSubscriptionBillingCycleEndDateSelector = mkSelector "setSubscriptionBillingCycleEndDate:"

-- | @Selector@ for @tierIdentifiers@
tierIdentifiersSelector :: Selector '[] (Id NSArray)
tierIdentifiersSelector = mkSelector "tierIdentifiers"

-- | @Selector@ for @setTierIdentifiers:@
setTierIdentifiersSelector :: Selector '[Id NSArray] ()
setTierIdentifiersSelector = mkSelector "setTierIdentifiers:"

-- | @Selector@ for @billingIdentifier@
billingIdentifierSelector :: Selector '[] (Id NSString)
billingIdentifierSelector = mkSelector "billingIdentifier"

-- | @Selector@ for @setBillingIdentifier:@
setBillingIdentifierSelector :: Selector '[Id NSString] ()
setBillingIdentifierSelector = mkSelector "setBillingIdentifier:"

-- | @Selector@ for @authenticationData@
authenticationDataSelector :: Selector '[] (Id NSString)
authenticationDataSelector = mkSelector "authenticationData"

-- | @Selector@ for @setAuthenticationData:@
setAuthenticationDataSelector :: Selector '[Id NSString] ()
setAuthenticationDataSelector = mkSelector "setAuthenticationData:"

-- | @Selector@ for @fromCurrentDevice@
fromCurrentDeviceSelector :: Selector '[] Bool
fromCurrentDeviceSelector = mkSelector "fromCurrentDevice"

-- | @Selector@ for @deviceCategory@
deviceCategorySelector :: Selector '[] VSOriginatingDeviceCategory
deviceCategorySelector = mkSelector "deviceCategory"

-- | @Selector@ for @appleSubscription@
appleSubscriptionSelector :: Selector '[] (Id VSAppleSubscription)
appleSubscriptionSelector = mkSelector "appleSubscription"

-- | @Selector@ for @setAppleSubscription:@
setAppleSubscriptionSelector :: Selector '[Id VSAppleSubscription] ()
setAppleSubscriptionSelector = mkSelector "setAppleSubscription:"

