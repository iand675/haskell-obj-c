{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSubscription@.
module ObjC.CloudKit.CKSubscription
  ( CKSubscription
  , IsCKSubscription(..)
  , init_
  , new
  , subscriptionID
  , subscriptionType
  , notificationInfo
  , setNotificationInfo
  , initSelector
  , newSelector
  , notificationInfoSelector
  , setNotificationInfoSelector
  , subscriptionIDSelector
  , subscriptionTypeSelector

  -- * Enum types
  , CKSubscriptionType(CKSubscriptionType)
  , pattern CKSubscriptionTypeQuery
  , pattern CKSubscriptionTypeRecordZone
  , pattern CKSubscriptionTypeDatabase

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

-- | @- init@
init_ :: IsCKSubscription ckSubscription => ckSubscription -> IO (Id CKSubscription)
init_ ckSubscription =
  sendOwnedMessage ckSubscription initSelector

-- | @+ new@
new :: IO (Id CKSubscription)
new  =
  do
    cls' <- getRequiredClass "CKSubscription"
    sendOwnedClassMessage cls' newSelector

-- | @- subscriptionID@
subscriptionID :: IsCKSubscription ckSubscription => ckSubscription -> IO (Id NSString)
subscriptionID ckSubscription =
  sendMessage ckSubscription subscriptionIDSelector

-- | @- subscriptionType@
subscriptionType :: IsCKSubscription ckSubscription => ckSubscription -> IO CKSubscriptionType
subscriptionType ckSubscription =
  sendMessage ckSubscription subscriptionTypeSelector

-- | Describes the notification that will be sent when the subscription fires.
--
-- This property must be set to a non-nil value before saving the @CKSubscription.@
--
-- ObjC selector: @- notificationInfo@
notificationInfo :: IsCKSubscription ckSubscription => ckSubscription -> IO (Id CKNotificationInfo)
notificationInfo ckSubscription =
  sendMessage ckSubscription notificationInfoSelector

-- | Describes the notification that will be sent when the subscription fires.
--
-- This property must be set to a non-nil value before saving the @CKSubscription.@
--
-- ObjC selector: @- setNotificationInfo:@
setNotificationInfo :: (IsCKSubscription ckSubscription, IsCKNotificationInfo value) => ckSubscription -> value -> IO ()
setNotificationInfo ckSubscription value =
  sendMessage ckSubscription setNotificationInfoSelector (toCKNotificationInfo value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSubscription)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSubscription)
newSelector = mkSelector "new"

-- | @Selector@ for @subscriptionID@
subscriptionIDSelector :: Selector '[] (Id NSString)
subscriptionIDSelector = mkSelector "subscriptionID"

-- | @Selector@ for @subscriptionType@
subscriptionTypeSelector :: Selector '[] CKSubscriptionType
subscriptionTypeSelector = mkSelector "subscriptionType"

-- | @Selector@ for @notificationInfo@
notificationInfoSelector :: Selector '[] (Id CKNotificationInfo)
notificationInfoSelector = mkSelector "notificationInfo"

-- | @Selector@ for @setNotificationInfo:@
setNotificationInfoSelector :: Selector '[Id CKNotificationInfo] ()
setNotificationInfoSelector = mkSelector "setNotificationInfo:"

