{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKRecordZoneSubscription
--
-- A subscription that fires whenever any change happens in the indicated Record Zone.
--
-- The RecordZone must have the capability @CKRecordZoneCapabilityFetchChanges@  @CKRecordZoneSubscriptions@ are not supported in a @sharedCloudDatabase@
--
-- Generated bindings for @CKRecordZoneSubscription@.
module ObjC.CloudKit.CKRecordZoneSubscription
  ( CKRecordZoneSubscription
  , IsCKRecordZoneSubscription(..)
  , initWithZoneID
  , initWithZoneID_subscriptionID
  , initWithCoder
  , zoneID
  , recordType
  , setRecordType
  , initWithCoderSelector
  , initWithZoneIDSelector
  , initWithZoneID_subscriptionIDSelector
  , recordTypeSelector
  , setRecordTypeSelector
  , zoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsCKRecordZoneID zoneID) => ckRecordZoneSubscription -> zoneID -> IO (Id CKRecordZoneSubscription)
initWithZoneID ckRecordZoneSubscription zoneID =
  sendOwnedMessage ckRecordZoneSubscription initWithZoneIDSelector (toCKRecordZoneID zoneID)

-- | @- initWithZoneID:subscriptionID:@
initWithZoneID_subscriptionID :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsCKRecordZoneID zoneID, IsNSString subscriptionID) => ckRecordZoneSubscription -> zoneID -> subscriptionID -> IO (Id CKRecordZoneSubscription)
initWithZoneID_subscriptionID ckRecordZoneSubscription zoneID subscriptionID =
  sendOwnedMessage ckRecordZoneSubscription initWithZoneID_subscriptionIDSelector (toCKRecordZoneID zoneID) (toNSString subscriptionID)

-- | @- initWithCoder:@
initWithCoder :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsNSCoder aDecoder) => ckRecordZoneSubscription -> aDecoder -> IO (Id CKRecordZoneSubscription)
initWithCoder ckRecordZoneSubscription aDecoder =
  sendOwnedMessage ckRecordZoneSubscription initWithCoderSelector (toNSCoder aDecoder)

-- | @- zoneID@
zoneID :: IsCKRecordZoneSubscription ckRecordZoneSubscription => ckRecordZoneSubscription -> IO (Id CKRecordZoneID)
zoneID ckRecordZoneSubscription =
  sendMessage ckRecordZoneSubscription zoneIDSelector

-- | Optional property. If set, a zone subscription is scoped to record changes for this record type
--
-- ObjC selector: @- recordType@
recordType :: IsCKRecordZoneSubscription ckRecordZoneSubscription => ckRecordZoneSubscription -> IO (Id NSString)
recordType ckRecordZoneSubscription =
  sendMessage ckRecordZoneSubscription recordTypeSelector

-- | Optional property. If set, a zone subscription is scoped to record changes for this record type
--
-- ObjC selector: @- setRecordType:@
setRecordType :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsNSString value) => ckRecordZoneSubscription -> value -> IO ()
setRecordType ckRecordZoneSubscription value =
  sendMessage ckRecordZoneSubscription setRecordTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector '[Id CKRecordZoneID] (Id CKRecordZoneSubscription)
initWithZoneIDSelector = mkSelector "initWithZoneID:"

-- | @Selector@ for @initWithZoneID:subscriptionID:@
initWithZoneID_subscriptionIDSelector :: Selector '[Id CKRecordZoneID, Id NSString] (Id CKRecordZoneSubscription)
initWithZoneID_subscriptionIDSelector = mkSelector "initWithZoneID:subscriptionID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CKRecordZoneSubscription)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @setRecordType:@
setRecordTypeSelector :: Selector '[Id NSString] ()
setRecordTypeSelector = mkSelector "setRecordType:"

