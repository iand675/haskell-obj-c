{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A VSSubscription instance describes the extent to which a subscriber has access to content.
--
-- Generated bindings for @VSSubscription@.
module ObjC.VideoSubscriberAccount.VSSubscription
  ( VSSubscription
  , IsVSSubscription(..)
  , expirationDate
  , setExpirationDate
  , accessLevel
  , setAccessLevel
  , tierIdentifiers
  , setTierIdentifiers
  , billingIdentifier
  , setBillingIdentifier
  , accessLevelSelector
  , billingIdentifierSelector
  , expirationDateSelector
  , setAccessLevelSelector
  , setBillingIdentifierSelector
  , setExpirationDateSelector
  , setTierIdentifiersSelector
  , tierIdentifiersSelector

  -- * Enum types
  , VSSubscriptionAccessLevel(VSSubscriptionAccessLevel)
  , pattern VSSubscriptionAccessLevelUnknown
  , pattern VSSubscriptionAccessLevelFreeWithAccount
  , pattern VSSubscriptionAccessLevelPaid

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

-- | After this point in time, the subscription will be considered inactive.
--
-- If the current subscription becomes inactive, the system will behave as though the user is not subscribed at all, i.e. as though the registration center's current subscription had been set to nil.
--
-- Defaults to distantFuture.
--
-- Providing a value is useful in a limited number of scenarios, e.g. when the a subscriber decides not to renew their subscription, you should provide an expiration date that corresponds to the point in time when the final billing cycle will end.
--
-- This might also be useful if the subscription only grants access to content that is time-limited, e.g. a single season of games for a sports league.
--
-- ObjC selector: @- expirationDate@
expirationDate :: IsVSSubscription vsSubscription => vsSubscription -> IO (Id NSDate)
expirationDate vsSubscription =
  sendMessage vsSubscription expirationDateSelector

-- | After this point in time, the subscription will be considered inactive.
--
-- If the current subscription becomes inactive, the system will behave as though the user is not subscribed at all, i.e. as though the registration center's current subscription had been set to nil.
--
-- Defaults to distantFuture.
--
-- Providing a value is useful in a limited number of scenarios, e.g. when the a subscriber decides not to renew their subscription, you should provide an expiration date that corresponds to the point in time when the final billing cycle will end.
--
-- This might also be useful if the subscription only grants access to content that is time-limited, e.g. a single season of games for a sports league.
--
-- ObjC selector: @- setExpirationDate:@
setExpirationDate :: (IsVSSubscription vsSubscription, IsNSDate value) => vsSubscription -> value -> IO ()
setExpirationDate vsSubscription value =
  sendMessage vsSubscription setExpirationDateSelector (toNSDate value)

-- | Describes the level of access the subscriber has to your catalog of content.
--
-- It is an error to provide a subscription with an unknown access level as the current subscription.  Instead, choose the access level that describes the content that the subscriber can play.
--
-- ObjC selector: @- accessLevel@
accessLevel :: IsVSSubscription vsSubscription => vsSubscription -> IO VSSubscriptionAccessLevel
accessLevel vsSubscription =
  sendMessage vsSubscription accessLevelSelector

-- | Describes the level of access the subscriber has to your catalog of content.
--
-- It is an error to provide a subscription with an unknown access level as the current subscription.  Instead, choose the access level that describes the content that the subscriber can play.
--
-- ObjC selector: @- setAccessLevel:@
setAccessLevel :: IsVSSubscription vsSubscription => vsSubscription -> VSSubscriptionAccessLevel -> IO ()
setAccessLevel vsSubscription value =
  sendMessage vsSubscription setAccessLevelSelector value

-- | Identifies a subset of content from your catalog that subscriber can play.
--
-- Only provide values that are used in your availability feed's tier restrictions.
--
-- ObjC selector: @- tierIdentifiers@
tierIdentifiers :: IsVSSubscription vsSubscription => vsSubscription -> IO (Id NSArray)
tierIdentifiers vsSubscription =
  sendMessage vsSubscription tierIdentifiersSelector

-- | Identifies a subset of content from your catalog that subscriber can play.
--
-- Only provide values that are used in your availability feed's tier restrictions.
--
-- ObjC selector: @- setTierIdentifiers:@
setTierIdentifiers :: (IsVSSubscription vsSubscription, IsNSArray value) => vsSubscription -> value -> IO ()
setTierIdentifiers vsSubscription value =
  sendMessage vsSubscription setTierIdentifiersSelector (toNSArray value)

-- | Identifies the billing group associated with the subscription.  May be used, for example, to restrict content availability based on the proximity of the billing address to a specific venue.
--
-- ObjC selector: @- billingIdentifier@
billingIdentifier :: IsVSSubscription vsSubscription => vsSubscription -> IO (Id NSString)
billingIdentifier vsSubscription =
  sendMessage vsSubscription billingIdentifierSelector

-- | Identifies the billing group associated with the subscription.  May be used, for example, to restrict content availability based on the proximity of the billing address to a specific venue.
--
-- ObjC selector: @- setBillingIdentifier:@
setBillingIdentifier :: (IsVSSubscription vsSubscription, IsNSString value) => vsSubscription -> value -> IO ()
setBillingIdentifier vsSubscription value =
  sendMessage vsSubscription setBillingIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector '[] (Id NSDate)
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector '[Id NSDate] ()
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @accessLevel@
accessLevelSelector :: Selector '[] VSSubscriptionAccessLevel
accessLevelSelector = mkSelector "accessLevel"

-- | @Selector@ for @setAccessLevel:@
setAccessLevelSelector :: Selector '[VSSubscriptionAccessLevel] ()
setAccessLevelSelector = mkSelector "setAccessLevel:"

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

