{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.VideoSubscriberAccount.Internal.Classes (
    module ObjC.VideoSubscriberAccount.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- VSAccountApplicationProvider ----------

-- | An object which provides an account provider to be added to the list of providers in your application.
-- 
-- Phantom type for @VSAccountApplicationProvider@.
data VSAccountApplicationProvider

instance IsObjCObject (Id VSAccountApplicationProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountApplicationProvider"

class IsNSObject a => IsVSAccountApplicationProvider a where
  toVSAccountApplicationProvider :: a -> Id VSAccountApplicationProvider

instance IsVSAccountApplicationProvider (Id VSAccountApplicationProvider) where
  toVSAccountApplicationProvider = unsafeCastId

instance IsNSObject (Id VSAccountApplicationProvider) where
  toNSObject = unsafeCastId

-- ---------- VSAccountManager ----------

-- | A VSAccountManager instance coordinates access to a subscriber's account.
-- 
-- Phantom type for @VSAccountManager@.
data VSAccountManager

instance IsObjCObject (Id VSAccountManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountManager"

class IsNSObject a => IsVSAccountManager a where
  toVSAccountManager :: a -> Id VSAccountManager

instance IsVSAccountManager (Id VSAccountManager) where
  toVSAccountManager = unsafeCastId

instance IsNSObject (Id VSAccountManager) where
  toNSObject = unsafeCastId

-- ---------- VSAccountManagerResult ----------

-- | Represents an in-flight request to an account manger.
-- 
-- Phantom type for @VSAccountManagerResult@.
data VSAccountManagerResult

instance IsObjCObject (Id VSAccountManagerResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountManagerResult"

class IsNSObject a => IsVSAccountManagerResult a where
  toVSAccountManagerResult :: a -> Id VSAccountManagerResult

instance IsVSAccountManagerResult (Id VSAccountManagerResult) where
  toVSAccountManagerResult = unsafeCastId

instance IsNSObject (Id VSAccountManagerResult) where
  toNSObject = unsafeCastId

-- ---------- VSAccountMetadata ----------

-- | A collection of information about a subscriber's account.
-- 
-- Phantom type for @VSAccountMetadata@.
data VSAccountMetadata

instance IsObjCObject (Id VSAccountMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountMetadata"

class IsNSObject a => IsVSAccountMetadata a where
  toVSAccountMetadata :: a -> Id VSAccountMetadata

instance IsVSAccountMetadata (Id VSAccountMetadata) where
  toVSAccountMetadata = unsafeCastId

instance IsNSObject (Id VSAccountMetadata) where
  toNSObject = unsafeCastId

-- ---------- VSAccountMetadataRequest ----------

-- | Specifies which information the app wants to obtain about the subscriber's account. You should only request the information you need to fulfill your contractual obligations.
-- 
-- Phantom type for @VSAccountMetadataRequest@.
data VSAccountMetadataRequest

instance IsObjCObject (Id VSAccountMetadataRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountMetadataRequest"

class IsNSObject a => IsVSAccountMetadataRequest a where
  toVSAccountMetadataRequest :: a -> Id VSAccountMetadataRequest

instance IsVSAccountMetadataRequest (Id VSAccountMetadataRequest) where
  toVSAccountMetadataRequest = unsafeCastId

instance IsNSObject (Id VSAccountMetadataRequest) where
  toNSObject = unsafeCastId

-- ---------- VSAccountProviderResponse ----------

-- | A value object that encapsulates the response given by an account provider.
-- 
-- Phantom type for @VSAccountProviderResponse@.
data VSAccountProviderResponse

instance IsObjCObject (Id VSAccountProviderResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAccountProviderResponse"

class IsNSObject a => IsVSAccountProviderResponse a where
  toVSAccountProviderResponse :: a -> Id VSAccountProviderResponse

instance IsVSAccountProviderResponse (Id VSAccountProviderResponse) where
  toVSAccountProviderResponse = unsafeCastId

instance IsNSObject (Id VSAccountProviderResponse) where
  toNSObject = unsafeCastId

-- ---------- VSAppleSubscription ----------

-- | Phantom type for @VSAppleSubscription@.
data VSAppleSubscription

instance IsObjCObject (Id VSAppleSubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAppleSubscription"

class IsNSObject a => IsVSAppleSubscription a where
  toVSAppleSubscription :: a -> Id VSAppleSubscription

instance IsVSAppleSubscription (Id VSAppleSubscription) where
  toVSAppleSubscription = unsafeCastId

instance IsNSObject (Id VSAppleSubscription) where
  toNSObject = unsafeCastId

-- ---------- VSAutoSignInToken ----------

-- | Phantom type for @VSAutoSignInToken@.
data VSAutoSignInToken

instance IsObjCObject (Id VSAutoSignInToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAutoSignInToken"

class IsNSObject a => IsVSAutoSignInToken a where
  toVSAutoSignInToken :: a -> Id VSAutoSignInToken

instance IsVSAutoSignInToken (Id VSAutoSignInToken) where
  toVSAutoSignInToken = unsafeCastId

instance IsNSObject (Id VSAutoSignInToken) where
  toNSObject = unsafeCastId

-- ---------- VSAutoSignInTokenUpdateContext ----------

-- | Context object used to update the auto sign in token. This object has to be obtained through a user consent flow using @-[VSUserAccountManager requestAutoSignInAuthorizationWithCompletionHandler:]@, then it is passed to @-[VSUserAccountManager updateAutoSignInToken:updateContext:completionHandler:]@
-- 
-- Phantom type for @VSAutoSignInTokenUpdateContext@.
data VSAutoSignInTokenUpdateContext

instance IsObjCObject (Id VSAutoSignInTokenUpdateContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSAutoSignInTokenUpdateContext"

class IsNSObject a => IsVSAutoSignInTokenUpdateContext a where
  toVSAutoSignInTokenUpdateContext :: a -> Id VSAutoSignInTokenUpdateContext

instance IsVSAutoSignInTokenUpdateContext (Id VSAutoSignInTokenUpdateContext) where
  toVSAutoSignInTokenUpdateContext = unsafeCastId

instance IsNSObject (Id VSAutoSignInTokenUpdateContext) where
  toNSObject = unsafeCastId

-- ---------- VSSubscription ----------

-- | A VSSubscription instance describes the extent to which a subscriber has access to content.
-- 
-- Phantom type for @VSSubscription@.
data VSSubscription

instance IsObjCObject (Id VSSubscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSSubscription"

class IsNSObject a => IsVSSubscription a where
  toVSSubscription :: a -> Id VSSubscription

instance IsVSSubscription (Id VSSubscription) where
  toVSSubscription = unsafeCastId

instance IsNSObject (Id VSSubscription) where
  toNSObject = unsafeCastId

-- ---------- VSSubscriptionRegistrationCenter ----------

-- | VSSubscriptionRegistrationCenter stores subscription information.
-- 
-- Phantom type for @VSSubscriptionRegistrationCenter@.
data VSSubscriptionRegistrationCenter

instance IsObjCObject (Id VSSubscriptionRegistrationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSSubscriptionRegistrationCenter"

class IsNSObject a => IsVSSubscriptionRegistrationCenter a where
  toVSSubscriptionRegistrationCenter :: a -> Id VSSubscriptionRegistrationCenter

instance IsVSSubscriptionRegistrationCenter (Id VSSubscriptionRegistrationCenter) where
  toVSSubscriptionRegistrationCenter = unsafeCastId

instance IsNSObject (Id VSSubscriptionRegistrationCenter) where
  toNSObject = unsafeCastId

-- ---------- VSUserAccount ----------

-- | Phantom type for @VSUserAccount@.
data VSUserAccount

instance IsObjCObject (Id VSUserAccount) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSUserAccount"

class IsNSObject a => IsVSUserAccount a where
  toVSUserAccount :: a -> Id VSUserAccount

instance IsVSUserAccount (Id VSUserAccount) where
  toVSUserAccount = unsafeCastId

instance IsNSObject (Id VSUserAccount) where
  toNSObject = unsafeCastId

-- ---------- VSUserAccountManager ----------

-- | Phantom type for @VSUserAccountManager@.
data VSUserAccountManager

instance IsObjCObject (Id VSUserAccountManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VSUserAccountManager"

class IsNSObject a => IsVSUserAccountManager a where
  toVSUserAccountManager :: a -> Id VSUserAccountManager

instance IsVSUserAccountManager (Id VSUserAccountManager) where
  toVSUserAccountManager = unsafeCastId

instance IsNSObject (Id VSUserAccountManager) where
  toNSObject = unsafeCastId
