{-# LANGUAGE PatternSynonyms #-}
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
  , expirationDateSelector
  , setExpirationDateSelector
  , accessLevelSelector
  , setAccessLevelSelector
  , tierIdentifiersSelector
  , setTierIdentifiersSelector
  , billingIdentifierSelector
  , setBillingIdentifierSelector

  -- * Enum types
  , VSSubscriptionAccessLevel(VSSubscriptionAccessLevel)
  , pattern VSSubscriptionAccessLevelUnknown
  , pattern VSSubscriptionAccessLevelFreeWithAccount
  , pattern VSSubscriptionAccessLevelPaid

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
expirationDate vsSubscription  =
  sendMsg vsSubscription (mkSelector "expirationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setExpirationDate vsSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsSubscription (mkSelector "setExpirationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Describes the level of access the subscriber has to your catalog of content.
--
-- It is an error to provide a subscription with an unknown access level as the current subscription.  Instead, choose the access level that describes the content that the subscriber can play.
--
-- ObjC selector: @- accessLevel@
accessLevel :: IsVSSubscription vsSubscription => vsSubscription -> IO VSSubscriptionAccessLevel
accessLevel vsSubscription  =
  fmap (coerce :: CLong -> VSSubscriptionAccessLevel) $ sendMsg vsSubscription (mkSelector "accessLevel") retCLong []

-- | Describes the level of access the subscriber has to your catalog of content.
--
-- It is an error to provide a subscription with an unknown access level as the current subscription.  Instead, choose the access level that describes the content that the subscriber can play.
--
-- ObjC selector: @- setAccessLevel:@
setAccessLevel :: IsVSSubscription vsSubscription => vsSubscription -> VSSubscriptionAccessLevel -> IO ()
setAccessLevel vsSubscription  value =
  sendMsg vsSubscription (mkSelector "setAccessLevel:") retVoid [argCLong (coerce value)]

-- | Identifies a subset of content from your catalog that subscriber can play.
--
-- Only provide values that are used in your availability feed's tier restrictions.
--
-- ObjC selector: @- tierIdentifiers@
tierIdentifiers :: IsVSSubscription vsSubscription => vsSubscription -> IO (Id NSArray)
tierIdentifiers vsSubscription  =
  sendMsg vsSubscription (mkSelector "tierIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifies a subset of content from your catalog that subscriber can play.
--
-- Only provide values that are used in your availability feed's tier restrictions.
--
-- ObjC selector: @- setTierIdentifiers:@
setTierIdentifiers :: (IsVSSubscription vsSubscription, IsNSArray value) => vsSubscription -> value -> IO ()
setTierIdentifiers vsSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsSubscription (mkSelector "setTierIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Identifies the billing group associated with the subscription.  May be used, for example, to restrict content availability based on the proximity of the billing address to a specific venue.
--
-- ObjC selector: @- billingIdentifier@
billingIdentifier :: IsVSSubscription vsSubscription => vsSubscription -> IO (Id NSString)
billingIdentifier vsSubscription  =
  sendMsg vsSubscription (mkSelector "billingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifies the billing group associated with the subscription.  May be used, for example, to restrict content availability based on the proximity of the billing address to a specific venue.
--
-- ObjC selector: @- setBillingIdentifier:@
setBillingIdentifier :: (IsVSSubscription vsSubscription, IsNSString value) => vsSubscription -> value -> IO ()
setBillingIdentifier vsSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg vsSubscription (mkSelector "setBillingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expirationDate@
expirationDateSelector :: Selector
expirationDateSelector = mkSelector "expirationDate"

-- | @Selector@ for @setExpirationDate:@
setExpirationDateSelector :: Selector
setExpirationDateSelector = mkSelector "setExpirationDate:"

-- | @Selector@ for @accessLevel@
accessLevelSelector :: Selector
accessLevelSelector = mkSelector "accessLevel"

-- | @Selector@ for @setAccessLevel:@
setAccessLevelSelector :: Selector
setAccessLevelSelector = mkSelector "setAccessLevel:"

-- | @Selector@ for @tierIdentifiers@
tierIdentifiersSelector :: Selector
tierIdentifiersSelector = mkSelector "tierIdentifiers"

-- | @Selector@ for @setTierIdentifiers:@
setTierIdentifiersSelector :: Selector
setTierIdentifiersSelector = mkSelector "setTierIdentifiers:"

-- | @Selector@ for @billingIdentifier@
billingIdentifierSelector :: Selector
billingIdentifierSelector = mkSelector "billingIdentifier"

-- | @Selector@ for @setBillingIdentifier:@
setBillingIdentifierSelector :: Selector
setBillingIdentifierSelector = mkSelector "setBillingIdentifier:"

