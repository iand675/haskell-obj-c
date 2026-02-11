{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ExternalAccessory.Internal.Classes (
    module ObjC.ExternalAccessory.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- UIViewController ----------

-- | Forward declaration of the UIViewController class.
-- 
-- Phantom type for @UIViewController@.
data UIViewController

instance IsObjCObject (Id UIViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "UIViewController"

class IsObjCObject a => IsUIViewController a where
  toUIViewController :: a -> Id UIViewController

instance IsUIViewController (Id UIViewController) where
  toUIViewController = unsafeCastId

-- ---------- EAAccessory ----------

-- | Phantom type for @EAAccessory@.
data EAAccessory

instance IsObjCObject (Id EAAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EAAccessory"

class IsNSObject a => IsEAAccessory a where
  toEAAccessory :: a -> Id EAAccessory

instance IsEAAccessory (Id EAAccessory) where
  toEAAccessory = unsafeCastId

instance IsNSObject (Id EAAccessory) where
  toNSObject = unsafeCastId

-- ---------- EAAccessoryManager ----------

-- | Phantom type for @EAAccessoryManager@.
data EAAccessoryManager

instance IsObjCObject (Id EAAccessoryManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EAAccessoryManager"

class IsNSObject a => IsEAAccessoryManager a where
  toEAAccessoryManager :: a -> Id EAAccessoryManager

instance IsEAAccessoryManager (Id EAAccessoryManager) where
  toEAAccessoryManager = unsafeCastId

instance IsNSObject (Id EAAccessoryManager) where
  toNSObject = unsafeCastId

-- ---------- EASession ----------

-- | Phantom type for @EASession@.
data EASession

instance IsObjCObject (Id EASession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EASession"

class IsNSObject a => IsEASession a where
  toEASession :: a -> Id EASession

instance IsEASession (Id EASession) where
  toEASession = unsafeCastId

instance IsNSObject (Id EASession) where
  toNSObject = unsafeCastId

-- ---------- EAWiFiUnconfiguredAccessory ----------

-- | Forward declaration of the EAWiFiUnconfiguredAccessory class.
-- 
-- Phantom type for @EAWiFiUnconfiguredAccessory@.
data EAWiFiUnconfiguredAccessory

instance IsObjCObject (Id EAWiFiUnconfiguredAccessory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EAWiFiUnconfiguredAccessory"

class IsNSObject a => IsEAWiFiUnconfiguredAccessory a where
  toEAWiFiUnconfiguredAccessory :: a -> Id EAWiFiUnconfiguredAccessory

instance IsEAWiFiUnconfiguredAccessory (Id EAWiFiUnconfiguredAccessory) where
  toEAWiFiUnconfiguredAccessory = unsafeCastId

instance IsNSObject (Id EAWiFiUnconfiguredAccessory) where
  toNSObject = unsafeCastId

-- ---------- EAWiFiUnconfiguredAccessoryBrowser ----------

-- | Interface for browsing unconfigured accessories
--
-- This class brokers access to the MFi Wireless Accessory Configuration (WAC) process.             This browser enables the application to scan for unconfigured accessories,             connect them to the user's Wi-Fi infrastructure and configure attributes of             the accessory.
-- 
-- Phantom type for @EAWiFiUnconfiguredAccessoryBrowser@.
data EAWiFiUnconfiguredAccessoryBrowser

instance IsObjCObject (Id EAWiFiUnconfiguredAccessoryBrowser) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "EAWiFiUnconfiguredAccessoryBrowser"

class IsNSObject a => IsEAWiFiUnconfiguredAccessoryBrowser a where
  toEAWiFiUnconfiguredAccessoryBrowser :: a -> Id EAWiFiUnconfiguredAccessoryBrowser

instance IsEAWiFiUnconfiguredAccessoryBrowser (Id EAWiFiUnconfiguredAccessoryBrowser) where
  toEAWiFiUnconfiguredAccessoryBrowser = unsafeCastId

instance IsNSObject (Id EAWiFiUnconfiguredAccessoryBrowser) where
  toNSObject = unsafeCastId
