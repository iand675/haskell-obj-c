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
  , initWithZoneIDSelector
  , initWithZoneID_subscriptionIDSelector
  , initWithCoderSelector
  , zoneIDSelector
  , recordTypeSelector
  , setRecordTypeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsCKRecordZoneID zoneID) => ckRecordZoneSubscription -> zoneID -> IO (Id CKRecordZoneSubscription)
initWithZoneID ckRecordZoneSubscription  zoneID =
withObjCPtr zoneID $ \raw_zoneID ->
    sendMsg ckRecordZoneSubscription (mkSelector "initWithZoneID:") (retPtr retVoid) [argPtr (castPtr raw_zoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithZoneID:subscriptionID:@
initWithZoneID_subscriptionID :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsCKRecordZoneID zoneID, IsNSString subscriptionID) => ckRecordZoneSubscription -> zoneID -> subscriptionID -> IO (Id CKRecordZoneSubscription)
initWithZoneID_subscriptionID ckRecordZoneSubscription  zoneID subscriptionID =
withObjCPtr zoneID $ \raw_zoneID ->
  withObjCPtr subscriptionID $ \raw_subscriptionID ->
      sendMsg ckRecordZoneSubscription (mkSelector "initWithZoneID:subscriptionID:") (retPtr retVoid) [argPtr (castPtr raw_zoneID :: Ptr ()), argPtr (castPtr raw_subscriptionID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsNSCoder aDecoder) => ckRecordZoneSubscription -> aDecoder -> IO (Id CKRecordZoneSubscription)
initWithCoder ckRecordZoneSubscription  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckRecordZoneSubscription (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- zoneID@
zoneID :: IsCKRecordZoneSubscription ckRecordZoneSubscription => ckRecordZoneSubscription -> IO (Id CKRecordZoneID)
zoneID ckRecordZoneSubscription  =
  sendMsg ckRecordZoneSubscription (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property. If set, a zone subscription is scoped to record changes for this record type
--
-- ObjC selector: @- recordType@
recordType :: IsCKRecordZoneSubscription ckRecordZoneSubscription => ckRecordZoneSubscription -> IO (Id NSString)
recordType ckRecordZoneSubscription  =
  sendMsg ckRecordZoneSubscription (mkSelector "recordType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property. If set, a zone subscription is scoped to record changes for this record type
--
-- ObjC selector: @- setRecordType:@
setRecordType :: (IsCKRecordZoneSubscription ckRecordZoneSubscription, IsNSString value) => ckRecordZoneSubscription -> value -> IO ()
setRecordType ckRecordZoneSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckRecordZoneSubscription (mkSelector "setRecordType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector
initWithZoneIDSelector = mkSelector "initWithZoneID:"

-- | @Selector@ for @initWithZoneID:subscriptionID:@
initWithZoneID_subscriptionIDSelector :: Selector
initWithZoneID_subscriptionIDSelector = mkSelector "initWithZoneID:subscriptionID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @setRecordType:@
setRecordTypeSelector :: Selector
setRecordTypeSelector = mkSelector "setRecordType:"

