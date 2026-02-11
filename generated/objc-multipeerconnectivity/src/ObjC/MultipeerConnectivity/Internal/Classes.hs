{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MultipeerConnectivity.Internal.Classes (
    module ObjC.MultipeerConnectivity.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- MCAdvertiserAssistant ----------

-- | Phantom type for @MCAdvertiserAssistant@.
data MCAdvertiserAssistant

instance IsObjCObject (Id MCAdvertiserAssistant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCAdvertiserAssistant"

class IsNSObject a => IsMCAdvertiserAssistant a where
  toMCAdvertiserAssistant :: a -> Id MCAdvertiserAssistant

instance IsMCAdvertiserAssistant (Id MCAdvertiserAssistant) where
  toMCAdvertiserAssistant = unsafeCastId

instance IsNSObject (Id MCAdvertiserAssistant) where
  toNSObject = unsafeCastId

-- ---------- MCNearbyServiceAdvertiser ----------

-- | Phantom type for @MCNearbyServiceAdvertiser@.
data MCNearbyServiceAdvertiser

instance IsObjCObject (Id MCNearbyServiceAdvertiser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCNearbyServiceAdvertiser"

class IsNSObject a => IsMCNearbyServiceAdvertiser a where
  toMCNearbyServiceAdvertiser :: a -> Id MCNearbyServiceAdvertiser

instance IsMCNearbyServiceAdvertiser (Id MCNearbyServiceAdvertiser) where
  toMCNearbyServiceAdvertiser = unsafeCastId

instance IsNSObject (Id MCNearbyServiceAdvertiser) where
  toNSObject = unsafeCastId

-- ---------- MCNearbyServiceBrowser ----------

-- | Phantom type for @MCNearbyServiceBrowser@.
data MCNearbyServiceBrowser

instance IsObjCObject (Id MCNearbyServiceBrowser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCNearbyServiceBrowser"

class IsNSObject a => IsMCNearbyServiceBrowser a where
  toMCNearbyServiceBrowser :: a -> Id MCNearbyServiceBrowser

instance IsMCNearbyServiceBrowser (Id MCNearbyServiceBrowser) where
  toMCNearbyServiceBrowser = unsafeCastId

instance IsNSObject (Id MCNearbyServiceBrowser) where
  toNSObject = unsafeCastId

-- ---------- MCPeerID ----------

-- | Phantom type for @MCPeerID@.
data MCPeerID

instance IsObjCObject (Id MCPeerID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCPeerID"

class IsNSObject a => IsMCPeerID a where
  toMCPeerID :: a -> Id MCPeerID

instance IsMCPeerID (Id MCPeerID) where
  toMCPeerID = unsafeCastId

instance IsNSObject (Id MCPeerID) where
  toNSObject = unsafeCastId

-- ---------- MCSession ----------

-- | Phantom type for @MCSession@.
data MCSession

instance IsObjCObject (Id MCSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCSession"

class IsNSObject a => IsMCSession a where
  toMCSession :: a -> Id MCSession

instance IsMCSession (Id MCSession) where
  toMCSession = unsafeCastId

instance IsNSObject (Id MCSession) where
  toNSObject = unsafeCastId

-- ---------- MCBrowserViewController ----------

-- | Phantom type for @MCBrowserViewController@.
data MCBrowserViewController

instance IsObjCObject (Id MCBrowserViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MCBrowserViewController"

class IsNSViewController a => IsMCBrowserViewController a where
  toMCBrowserViewController :: a -> Id MCBrowserViewController

instance IsMCBrowserViewController (Id MCBrowserViewController) where
  toMCBrowserViewController = unsafeCastId

instance IsNSObject (Id MCBrowserViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MCBrowserViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id MCBrowserViewController) where
  toNSViewController = unsafeCastId
