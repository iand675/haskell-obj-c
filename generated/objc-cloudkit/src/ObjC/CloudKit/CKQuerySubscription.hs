{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRecordType_predicate_optionsSelector
  , initWithRecordType_predicate_subscriptionID_optionsSelector
  , initWithCoderSelector
  , recordTypeSelector
  , predicateSelector
  , zoneIDSelector
  , setZoneIDSelector
  , querySubscriptionOptionsSelector

  -- * Enum types
  , CKQuerySubscriptionOptions(CKQuerySubscriptionOptions)
  , pattern CKQuerySubscriptionOptionsFiresOnRecordCreation
  , pattern CKQuerySubscriptionOptionsFiresOnRecordUpdate
  , pattern CKQuerySubscriptionOptionsFiresOnRecordDeletion
  , pattern CKQuerySubscriptionOptionsFiresOnce

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

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRecordType:predicate:options:@
initWithRecordType_predicate_options :: (IsCKQuerySubscription ckQuerySubscription, IsNSString recordType, IsNSPredicate predicate) => ckQuerySubscription -> recordType -> predicate -> CKQuerySubscriptionOptions -> IO (Id CKQuerySubscription)
initWithRecordType_predicate_options ckQuerySubscription  recordType predicate querySubscriptionOptions =
withObjCPtr recordType $ \raw_recordType ->
  withObjCPtr predicate $ \raw_predicate ->
      sendMsg ckQuerySubscription (mkSelector "initWithRecordType:predicate:options:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ()), argCULong (coerce querySubscriptionOptions)] >>= ownedObject . castPtr

-- | @- initWithRecordType:predicate:subscriptionID:options:@
initWithRecordType_predicate_subscriptionID_options :: (IsCKQuerySubscription ckQuerySubscription, IsNSString recordType, IsNSPredicate predicate, IsNSString subscriptionID) => ckQuerySubscription -> recordType -> predicate -> subscriptionID -> CKQuerySubscriptionOptions -> IO (Id CKQuerySubscription)
initWithRecordType_predicate_subscriptionID_options ckQuerySubscription  recordType predicate subscriptionID querySubscriptionOptions =
withObjCPtr recordType $ \raw_recordType ->
  withObjCPtr predicate $ \raw_predicate ->
    withObjCPtr subscriptionID $ \raw_subscriptionID ->
        sendMsg ckQuerySubscription (mkSelector "initWithRecordType:predicate:subscriptionID:options:") (retPtr retVoid) [argPtr (castPtr raw_recordType :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr raw_subscriptionID :: Ptr ()), argCULong (coerce querySubscriptionOptions)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKQuerySubscription ckQuerySubscription, IsNSCoder aDecoder) => ckQuerySubscription -> aDecoder -> IO (Id CKQuerySubscription)
initWithCoder ckQuerySubscription  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckQuerySubscription (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | The record type that this subscription watches
--
-- ObjC selector: @- recordType@
recordType :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id NSString)
recordType ckQuerySubscription  =
  sendMsg ckQuerySubscription (mkSelector "recordType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A predicate that determines when the subscription fires.
--
-- ObjC selector: @- predicate@
predicate :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id NSPredicate)
predicate ckQuerySubscription  =
  sendMsg ckQuerySubscription (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property.  If set, a query subscription is scoped to only record changes in the indicated zone.  Query Subscriptions that do not specify a @zoneID@ are scoped to record changes across all zones in the database.
--
-- ObjC selector: @- zoneID@
zoneID :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO (Id CKRecordZoneID)
zoneID ckQuerySubscription  =
  sendMsg ckQuerySubscription (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property.  If set, a query subscription is scoped to only record changes in the indicated zone.  Query Subscriptions that do not specify a @zoneID@ are scoped to record changes across all zones in the database.
--
-- ObjC selector: @- setZoneID:@
setZoneID :: (IsCKQuerySubscription ckQuerySubscription, IsCKRecordZoneID value) => ckQuerySubscription -> value -> IO ()
setZoneID ckQuerySubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQuerySubscription (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Options flags describing the firing behavior subscription.
--
-- One of  @CKQuerySubscriptionOptionsFiresOnRecordCreation,@  @CKQuerySubscriptionOptionsFiresOnRecordUpdate,@ or  @CKQuerySubscriptionOptionsFiresOnRecordDeletion@ must be specified or an @NSInvalidArgumentException@ will be thrown.
--
-- ObjC selector: @- querySubscriptionOptions@
querySubscriptionOptions :: IsCKQuerySubscription ckQuerySubscription => ckQuerySubscription -> IO CKQuerySubscriptionOptions
querySubscriptionOptions ckQuerySubscription  =
  fmap (coerce :: CULong -> CKQuerySubscriptionOptions) $ sendMsg ckQuerySubscription (mkSelector "querySubscriptionOptions") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecordType:predicate:options:@
initWithRecordType_predicate_optionsSelector :: Selector
initWithRecordType_predicate_optionsSelector = mkSelector "initWithRecordType:predicate:options:"

-- | @Selector@ for @initWithRecordType:predicate:subscriptionID:options:@
initWithRecordType_predicate_subscriptionID_optionsSelector :: Selector
initWithRecordType_predicate_subscriptionID_optionsSelector = mkSelector "initWithRecordType:predicate:subscriptionID:options:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @querySubscriptionOptions@
querySubscriptionOptionsSelector :: Selector
querySubscriptionOptionsSelector = mkSelector "querySubscriptionOptions"

