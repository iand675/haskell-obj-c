{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKDatabaseSubscription
--
-- A subscription fires whenever any change happens in the database that this subscription was saved in.
--
-- @CKDatabaseSubscription@ is only supported in the Private and Shared databases.
--
-- Generated bindings for @CKDatabaseSubscription@.
module ObjC.CloudKit.CKDatabaseSubscription
  ( CKDatabaseSubscription
  , IsCKDatabaseSubscription(..)
  , init_
  , new
  , initWithSubscriptionID
  , initWithCoder
  , recordType
  , setRecordType
  , initSelector
  , newSelector
  , initWithSubscriptionIDSelector
  , initWithCoderSelector
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

-- | @- init@
init_ :: IsCKDatabaseSubscription ckDatabaseSubscription => ckDatabaseSubscription -> IO (Id CKDatabaseSubscription)
init_ ckDatabaseSubscription  =
  sendMsg ckDatabaseSubscription (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKDatabaseSubscription)
new  =
  do
    cls' <- getRequiredClass "CKDatabaseSubscription"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSubscriptionID:@
initWithSubscriptionID :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSString subscriptionID) => ckDatabaseSubscription -> subscriptionID -> IO (Id CKDatabaseSubscription)
initWithSubscriptionID ckDatabaseSubscription  subscriptionID =
withObjCPtr subscriptionID $ \raw_subscriptionID ->
    sendMsg ckDatabaseSubscription (mkSelector "initWithSubscriptionID:") (retPtr retVoid) [argPtr (castPtr raw_subscriptionID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSCoder aDecoder) => ckDatabaseSubscription -> aDecoder -> IO (Id CKDatabaseSubscription)
initWithCoder ckDatabaseSubscription  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg ckDatabaseSubscription (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | Optional property. If set, a database subscription is scoped to record changes for this record type
--
-- ObjC selector: @- recordType@
recordType :: IsCKDatabaseSubscription ckDatabaseSubscription => ckDatabaseSubscription -> IO (Id NSString)
recordType ckDatabaseSubscription  =
  sendMsg ckDatabaseSubscription (mkSelector "recordType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional property. If set, a database subscription is scoped to record changes for this record type
--
-- ObjC selector: @- setRecordType:@
setRecordType :: (IsCKDatabaseSubscription ckDatabaseSubscription, IsNSString value) => ckDatabaseSubscription -> value -> IO ()
setRecordType ckDatabaseSubscription  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckDatabaseSubscription (mkSelector "setRecordType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithSubscriptionID:@
initWithSubscriptionIDSelector :: Selector
initWithSubscriptionIDSelector = mkSelector "initWithSubscriptionID:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector
recordTypeSelector = mkSelector "recordType"

-- | @Selector@ for @setRecordType:@
setRecordTypeSelector :: Selector
setRecordTypeSelector = mkSelector "setRecordType:"

