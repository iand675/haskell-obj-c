{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKQuerySubscription
--
-- A subscription that fires whenever a change matching the predicate occurs.
--
-- @CKQuerySubscriptions@ are not supported in a @sharedCloudDatabase@
--
-- Generated bindings for @CKQuerySubscription@.
module ObjC.CloudKit.CKQuerySubscription
  ( CKQuerySubscription
  , IsCKQuerySubscription(..)
  , initWithRecordType_predicate_options
  , initWithRecordType_predicate_subscriptionID_options
  , initWithCoder
  , recordType
  , predicate
  , zoneID
  , setZoneID
  , querySubscriptionOptions
  , initWithCoderSelector
  , initWithRecordType_predicate_optionsSelector
  , initWithRecordType_predicate_subscriptionID_optionsSelector
  , predicateSelector
  , querySubscriptionOptionsSelector
  , recordTypeSelector
  , setZoneIDSelector
  , zoneIDSelector

  -- * Enum types
  , CKQuerySubscriptionOptions(CKQuerySubscriptionOptions)
  , pattern CKQuerySubscriptionOptionsFiresOnRecordCreation
  , pattern CKQuerySubscriptionOptionsFiresOnRecordUpdate
  , pattern CKQuerySubscriptionOptionsFiresOnRecordDeletion
  , pattern CKQuerySubscriptionOptionsFiresOnce

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRecordType:predicate:options:@
initWithRecordType_predicate_options :: (IsCKQuerySubscription ckQuerySubscription, IsNSString recordType, IsNSPredicate predicate) => ckQuerySubscription -> recordType -> predicate -> CKQuerySubscriptionOptions -> IO (Id CKQuerySubscription)
initWithRecordType_predicate_options ckQuerySubscription recordType predicate querySubscriptionOptions =
  sendOwnedMessage ckQuerySubscription initWithRecordType_predicate_optionsSelector (toNSString recordType) (toNSPredicate predicate) querySubscriptionOptions

-- | @- initWithRecordType:predicate:subscriptionID:options:@
initWithRecordType_predicate_subscriptionID_options :: (IsCKQuerySubscription ckQuerySubscription, IsNSString recordType, IsNSPredicate predicate, IsNSString subscriptionID) => ckQuerySubscription -> recordType -> predicate -> subscriptionID -> CKQuerySubscriptionOptions -> IO (Id CKQuerySubscription)
initWithRecordType_predicate_subscriptionID_options ckQuerySubscription recordType predicate subscriptionID querySubscriptionOptions =
  sendOwnedMessage ckQuerySubscription initWithRecordType_predicate_subscriptionID_optionsSelector (toNSString recordType) (toNSPredicate predicate) (toNSString subscriptionID) querySubscriptionOptions

-- | @- initWithCoder:@
initWithCoder :: (IsCKQuerySubscription ckQuerySubscription, IsNSCoder aDecoder) => ckQuerySubscription -> aDecoder -> IO (Id CKQuerySubscription)
initWithCoder ckQuerySubscription aDecoder =
  sendOwnedMessage ckQuerySubscription initWithCoderSelector (toNSCoder aDecoder)

-- | The record type that this subscription watches
--
-- ObjC selector: @- recordType@
recordType :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id NSString)
recordType ckQuerySubscription =
  sendMessage ckQuerySubscription recordTypeSelector

-- | A predicate that determines when the subscription fires.
--
-- ObjC selector: @- predicate@
predicate :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id NSPredicate)
predicate ckQuerySubscription =
  sendMessage ckQuerySubscription predicateSelector

-- | Optional property.  If set, a query subscription is scoped to only record changes in the indicated zone.  Query Subscriptions that do not specify a @zoneID@ are scoped to record changes across all zones in the database.
--
-- ObjC selector: @- zoneID@
zoneID :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id CKRecordZoneID)
zoneID ckQuerySubscription =
  sendMessage ckQuerySubscription zoneIDSelector

-- | Optional property.  If set, a query subscription is scoped to only record changes in the indicated zone.  Query Subscriptions that do not specify a @zoneID@ are scoped to record changes across all zones in the database.
--
-- ObjC selector: @- setZoneID:@
setZoneID :: (IsCKQuerySubscription ckQuerySubscription, IsCKRecordZoneID value) => ckQuerySubscription -> value -> IO ()
setZoneID ckQuerySubscription value =
  sendMessage ckQuerySubscription setZoneIDSelector (toCKRecordZoneID value)

-- | Options flags describing the firing behavior subscription.
--
-- One of  @CKQuerySubscriptionOptionsFiresOnRecordCreation,@  @CKQuerySubscriptionOptionsFiresOnRecordUpdate,@ or  @CKQuerySubscriptionOptionsFiresOnRecordDeletion@ must be specified or an @NSInvalidArgumentException@ will be thrown.
--
-- ObjC selector: @- querySubscriptionOptions@
querySubscriptionOptions :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO CKQuerySubscriptionOptions
querySubscriptionOptions ckQuerySubscription =
  sendMessage ckQuerySubscription querySubscriptionOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecordType:predicate:options:@
initWithRecordType_predicate_optionsSelector :: Selector '[Id NSString, Id NSPredicate, CKQuerySubscriptionOptions] (Id CKQuerySubscription)
initWithRecordType_predicate_optionsSelector = mkSelector "initWithRecordType:predicate:options:"

-- | @Selector@ for @initWithRecordType:predicate:subscriptionID:options:@
initWithRecordType_predicate_subscriptionID_optionsSelector :: Selector '[Id NSString, Id NSPredicate, Id NSString, CKQuerySubscriptionOptions] (Id CKQuerySubscription)
initWithRecordType_predicate_subscriptionID_optionsSelector = mkSelector "initWithRecordType:predicate:subscriptionID:options:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CKQuerySubscription)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id CKRecordZoneID] ()
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @querySubscriptionOptions@
querySubscriptionOptionsSelector :: Selector '[] CKQuerySubscriptionOptions
querySubscriptionOptionsSelector = mkSelector "querySubscriptionOptions"

