{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.IdentityLookup.Internal.Classes (
    module ObjC.IdentityLookup.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- ILClassificationRequest ----------

-- | A request to classify a communication.
-- 
-- Phantom type for @ILClassificationRequest@.
data ILClassificationRequest

instance IsObjCObject (Id ILClassificationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILClassificationRequest"

class IsNSObject a => IsILClassificationRequest a where
  toILClassificationRequest :: a -> Id ILClassificationRequest

instance IsILClassificationRequest (Id ILClassificationRequest) where
  toILClassificationRequest = unsafeCastId

instance IsNSObject (Id ILClassificationRequest) where
  toNSObject = unsafeCastId

-- ---------- ILClassificationResponse ----------

-- | A response to an ILClassificationRequest.
-- 
-- Phantom type for @ILClassificationResponse@.
data ILClassificationResponse

instance IsObjCObject (Id ILClassificationResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILClassificationResponse"

class IsNSObject a => IsILClassificationResponse a where
  toILClassificationResponse :: a -> Id ILClassificationResponse

instance IsILClassificationResponse (Id ILClassificationResponse) where
  toILClassificationResponse = unsafeCastId

instance IsNSObject (Id ILClassificationResponse) where
  toNSObject = unsafeCastId

-- ---------- ILCommunication ----------

-- | An incident of communication via some medium.
-- 
-- Phantom type for @ILCommunication@.
data ILCommunication

instance IsObjCObject (Id ILCommunication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILCommunication"

class IsNSObject a => IsILCommunication a where
  toILCommunication :: a -> Id ILCommunication

instance IsILCommunication (Id ILCommunication) where
  toILCommunication = unsafeCastId

instance IsNSObject (Id ILCommunication) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterCapabilitiesQueryRequest ----------

-- | A request to query a MessageFilter extension about how to interpret a received message.
-- 
-- Phantom type for @ILMessageFilterCapabilitiesQueryRequest@.
data ILMessageFilterCapabilitiesQueryRequest

instance IsObjCObject (Id ILMessageFilterCapabilitiesQueryRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterCapabilitiesQueryRequest"

class IsNSObject a => IsILMessageFilterCapabilitiesQueryRequest a where
  toILMessageFilterCapabilitiesQueryRequest :: a -> Id ILMessageFilterCapabilitiesQueryRequest

instance IsILMessageFilterCapabilitiesQueryRequest (Id ILMessageFilterCapabilitiesQueryRequest) where
  toILMessageFilterCapabilitiesQueryRequest = unsafeCastId

instance IsNSObject (Id ILMessageFilterCapabilitiesQueryRequest) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterCapabilitiesQueryResponse ----------

-- | A response to an ILMessageFilterCapabilitiesQueryRequest.
-- 
-- Phantom type for @ILMessageFilterCapabilitiesQueryResponse@.
data ILMessageFilterCapabilitiesQueryResponse

instance IsObjCObject (Id ILMessageFilterCapabilitiesQueryResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterCapabilitiesQueryResponse"

class IsNSObject a => IsILMessageFilterCapabilitiesQueryResponse a where
  toILMessageFilterCapabilitiesQueryResponse :: a -> Id ILMessageFilterCapabilitiesQueryResponse

instance IsILMessageFilterCapabilitiesQueryResponse (Id ILMessageFilterCapabilitiesQueryResponse) where
  toILMessageFilterCapabilitiesQueryResponse = unsafeCastId

instance IsNSObject (Id ILMessageFilterCapabilitiesQueryResponse) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterExtension ----------

-- | Parent class for a MessageFilter extension's principal class.
-- 
-- Phantom type for @ILMessageFilterExtension@.
data ILMessageFilterExtension

instance IsObjCObject (Id ILMessageFilterExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterExtension"

class IsNSObject a => IsILMessageFilterExtension a where
  toILMessageFilterExtension :: a -> Id ILMessageFilterExtension

instance IsILMessageFilterExtension (Id ILMessageFilterExtension) where
  toILMessageFilterExtension = unsafeCastId

instance IsNSObject (Id ILMessageFilterExtension) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterQueryRequest ----------

-- | A request to query a MessageFilter extension about how to interpret a received message.
-- 
-- Phantom type for @ILMessageFilterQueryRequest@.
data ILMessageFilterQueryRequest

instance IsObjCObject (Id ILMessageFilterQueryRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterQueryRequest"

class IsNSObject a => IsILMessageFilterQueryRequest a where
  toILMessageFilterQueryRequest :: a -> Id ILMessageFilterQueryRequest

instance IsILMessageFilterQueryRequest (Id ILMessageFilterQueryRequest) where
  toILMessageFilterQueryRequest = unsafeCastId

instance IsNSObject (Id ILMessageFilterQueryRequest) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterQueryResponse ----------

-- | A response to an ILMessageFilterQueryRequest.
-- 
-- Phantom type for @ILMessageFilterQueryResponse@.
data ILMessageFilterQueryResponse

instance IsObjCObject (Id ILMessageFilterQueryResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterQueryResponse"

class IsNSObject a => IsILMessageFilterQueryResponse a where
  toILMessageFilterQueryResponse :: a -> Id ILMessageFilterQueryResponse

instance IsILMessageFilterQueryResponse (Id ILMessageFilterQueryResponse) where
  toILMessageFilterQueryResponse = unsafeCastId

instance IsNSObject (Id ILMessageFilterQueryResponse) where
  toNSObject = unsafeCastId

-- ---------- ILNetworkResponse ----------

-- | A response to an HTTPS network request.
-- 
-- Phantom type for @ILNetworkResponse@.
data ILNetworkResponse

instance IsObjCObject (Id ILNetworkResponse) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILNetworkResponse"

class IsNSObject a => IsILNetworkResponse a where
  toILNetworkResponse :: a -> Id ILNetworkResponse

instance IsILNetworkResponse (Id ILNetworkResponse) where
  toILNetworkResponse = unsafeCastId

instance IsNSObject (Id ILNetworkResponse) where
  toNSObject = unsafeCastId

-- ---------- ILCallClassificationRequest ----------

-- | Phantom type for @ILCallClassificationRequest@.
data ILCallClassificationRequest

instance IsObjCObject (Id ILCallClassificationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILCallClassificationRequest"

class IsILClassificationRequest a => IsILCallClassificationRequest a where
  toILCallClassificationRequest :: a -> Id ILCallClassificationRequest

instance IsILCallClassificationRequest (Id ILCallClassificationRequest) where
  toILCallClassificationRequest = unsafeCastId

instance IsILClassificationRequest (Id ILCallClassificationRequest) where
  toILClassificationRequest = unsafeCastId

instance IsNSObject (Id ILCallClassificationRequest) where
  toNSObject = unsafeCastId

-- ---------- ILMessageClassificationRequest ----------

-- | Phantom type for @ILMessageClassificationRequest@.
data ILMessageClassificationRequest

instance IsObjCObject (Id ILMessageClassificationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageClassificationRequest"

class IsILClassificationRequest a => IsILMessageClassificationRequest a where
  toILMessageClassificationRequest :: a -> Id ILMessageClassificationRequest

instance IsILMessageClassificationRequest (Id ILMessageClassificationRequest) where
  toILMessageClassificationRequest = unsafeCastId

instance IsILClassificationRequest (Id ILMessageClassificationRequest) where
  toILClassificationRequest = unsafeCastId

instance IsNSObject (Id ILMessageClassificationRequest) where
  toNSObject = unsafeCastId

-- ---------- ILCallCommunication ----------

-- | Phantom type for @ILCallCommunication@.
data ILCallCommunication

instance IsObjCObject (Id ILCallCommunication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILCallCommunication"

class IsILCommunication a => IsILCallCommunication a where
  toILCallCommunication :: a -> Id ILCallCommunication

instance IsILCallCommunication (Id ILCallCommunication) where
  toILCallCommunication = unsafeCastId

instance IsILCommunication (Id ILCallCommunication) where
  toILCommunication = unsafeCastId

instance IsNSObject (Id ILCallCommunication) where
  toNSObject = unsafeCastId

-- ---------- ILMessageCommunication ----------

-- | Phantom type for @ILMessageCommunication@.
data ILMessageCommunication

instance IsObjCObject (Id ILMessageCommunication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageCommunication"

class IsILCommunication a => IsILMessageCommunication a where
  toILMessageCommunication :: a -> Id ILMessageCommunication

instance IsILMessageCommunication (Id ILMessageCommunication) where
  toILMessageCommunication = unsafeCastId

instance IsILCommunication (Id ILMessageCommunication) where
  toILCommunication = unsafeCastId

instance IsNSObject (Id ILMessageCommunication) where
  toNSObject = unsafeCastId

-- ---------- ILMessageFilterExtensionContext ----------

-- | Represents a MessageFilter extension request's context.
-- 
-- Phantom type for @ILMessageFilterExtensionContext@.
data ILMessageFilterExtensionContext

instance IsObjCObject (Id ILMessageFilterExtensionContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ILMessageFilterExtensionContext"

class IsNSExtensionContext a => IsILMessageFilterExtensionContext a where
  toILMessageFilterExtensionContext :: a -> Id ILMessageFilterExtensionContext

instance IsILMessageFilterExtensionContext (Id ILMessageFilterExtensionContext) where
  toILMessageFilterExtensionContext = unsafeCastId

instance IsNSExtensionContext (Id ILMessageFilterExtensionContext) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id ILMessageFilterExtensionContext) where
  toNSObject = unsafeCastId
