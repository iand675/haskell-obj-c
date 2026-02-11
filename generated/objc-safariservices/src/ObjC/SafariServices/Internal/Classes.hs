{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SafariServices.Internal.Classes (
    module ObjC.SafariServices.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SFContentBlockerManager ----------

-- | Phantom type for @SFContentBlockerManager@.
data SFContentBlockerManager

instance IsObjCObject (Id SFContentBlockerManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFContentBlockerManager"

class IsNSObject a => IsSFContentBlockerManager a where
  toSFContentBlockerManager :: a -> Id SFContentBlockerManager

instance IsSFContentBlockerManager (Id SFContentBlockerManager) where
  toSFContentBlockerManager = unsafeCastId

instance IsNSObject (Id SFContentBlockerManager) where
  toNSObject = unsafeCastId

-- ---------- SFContentBlockerState ----------

-- | Phantom type for @SFContentBlockerState@.
data SFContentBlockerState

instance IsObjCObject (Id SFContentBlockerState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFContentBlockerState"

class IsNSObject a => IsSFContentBlockerState a where
  toSFContentBlockerState :: a -> Id SFContentBlockerState

instance IsSFContentBlockerState (Id SFContentBlockerState) where
  toSFContentBlockerState = unsafeCastId

instance IsNSObject (Id SFContentBlockerState) where
  toNSObject = unsafeCastId

-- ---------- SFSafariApplication ----------

-- | Phantom type for @SFSafariApplication@.
data SFSafariApplication

instance IsObjCObject (Id SFSafariApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariApplication"

class IsNSObject a => IsSFSafariApplication a where
  toSFSafariApplication :: a -> Id SFSafariApplication

instance IsSFSafariApplication (Id SFSafariApplication) where
  toSFSafariApplication = unsafeCastId

instance IsNSObject (Id SFSafariApplication) where
  toNSObject = unsafeCastId

-- ---------- SFSafariExtension ----------

-- | Phantom type for @SFSafariExtension@.
data SFSafariExtension

instance IsObjCObject (Id SFSafariExtension) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariExtension"

class IsNSObject a => IsSFSafariExtension a where
  toSFSafariExtension :: a -> Id SFSafariExtension

instance IsSFSafariExtension (Id SFSafariExtension) where
  toSFSafariExtension = unsafeCastId

instance IsNSObject (Id SFSafariExtension) where
  toNSObject = unsafeCastId

-- ---------- SFSafariExtensionHandler ----------

-- | Phantom type for @SFSafariExtensionHandler@.
data SFSafariExtensionHandler

instance IsObjCObject (Id SFSafariExtensionHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariExtensionHandler"

class IsNSObject a => IsSFSafariExtensionHandler a where
  toSFSafariExtensionHandler :: a -> Id SFSafariExtensionHandler

instance IsSFSafariExtensionHandler (Id SFSafariExtensionHandler) where
  toSFSafariExtensionHandler = unsafeCastId

instance IsNSObject (Id SFSafariExtensionHandler) where
  toNSObject = unsafeCastId

-- ---------- SFSafariExtensionManager ----------

-- | Phantom type for @SFSafariExtensionManager@.
data SFSafariExtensionManager

instance IsObjCObject (Id SFSafariExtensionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariExtensionManager"

class IsNSObject a => IsSFSafariExtensionManager a where
  toSFSafariExtensionManager :: a -> Id SFSafariExtensionManager

instance IsSFSafariExtensionManager (Id SFSafariExtensionManager) where
  toSFSafariExtensionManager = unsafeCastId

instance IsNSObject (Id SFSafariExtensionManager) where
  toNSObject = unsafeCastId

-- ---------- SFSafariExtensionState ----------

-- | Phantom type for @SFSafariExtensionState@.
data SFSafariExtensionState

instance IsObjCObject (Id SFSafariExtensionState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariExtensionState"

class IsNSObject a => IsSFSafariExtensionState a where
  toSFSafariExtensionState :: a -> Id SFSafariExtensionState

instance IsSFSafariExtensionState (Id SFSafariExtensionState) where
  toSFSafariExtensionState = unsafeCastId

instance IsNSObject (Id SFSafariExtensionState) where
  toNSObject = unsafeCastId

-- ---------- SFSafariPage ----------

-- | Phantom type for @SFSafariPage@.
data SFSafariPage

instance IsObjCObject (Id SFSafariPage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariPage"

class IsNSObject a => IsSFSafariPage a where
  toSFSafariPage :: a -> Id SFSafariPage

instance IsSFSafariPage (Id SFSafariPage) where
  toSFSafariPage = unsafeCastId

instance IsNSObject (Id SFSafariPage) where
  toNSObject = unsafeCastId

-- ---------- SFSafariPageProperties ----------

-- | Phantom type for @SFSafariPageProperties@.
data SFSafariPageProperties

instance IsObjCObject (Id SFSafariPageProperties) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariPageProperties"

class IsNSObject a => IsSFSafariPageProperties a where
  toSFSafariPageProperties :: a -> Id SFSafariPageProperties

instance IsSFSafariPageProperties (Id SFSafariPageProperties) where
  toSFSafariPageProperties = unsafeCastId

instance IsNSObject (Id SFSafariPageProperties) where
  toNSObject = unsafeCastId

-- ---------- SFSafariTab ----------

-- | Phantom type for @SFSafariTab@.
data SFSafariTab

instance IsObjCObject (Id SFSafariTab) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariTab"

class IsNSObject a => IsSFSafariTab a where
  toSFSafariTab :: a -> Id SFSafariTab

instance IsSFSafariTab (Id SFSafariTab) where
  toSFSafariTab = unsafeCastId

instance IsNSObject (Id SFSafariTab) where
  toNSObject = unsafeCastId

-- ---------- SFSafariToolbarItem ----------

-- | Phantom type for @SFSafariToolbarItem@.
data SFSafariToolbarItem

instance IsObjCObject (Id SFSafariToolbarItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariToolbarItem"

class IsNSObject a => IsSFSafariToolbarItem a where
  toSFSafariToolbarItem :: a -> Id SFSafariToolbarItem

instance IsSFSafariToolbarItem (Id SFSafariToolbarItem) where
  toSFSafariToolbarItem = unsafeCastId

instance IsNSObject (Id SFSafariToolbarItem) where
  toNSObject = unsafeCastId

-- ---------- SFSafariWindow ----------

-- | Phantom type for @SFSafariWindow@.
data SFSafariWindow

instance IsObjCObject (Id SFSafariWindow) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariWindow"

class IsNSObject a => IsSFSafariWindow a where
  toSFSafariWindow :: a -> Id SFSafariWindow

instance IsSFSafariWindow (Id SFSafariWindow) where
  toSFSafariWindow = unsafeCastId

instance IsNSObject (Id SFSafariWindow) where
  toNSObject = unsafeCastId

-- ---------- SFUniversalLink ----------

-- | This class represents a universal link available on the current system. Universal links can be opened in a browser or directly in an application.
--
-- Warning: The use of this class requires an entitlement.
-- 
-- Phantom type for @SFUniversalLink@.
data SFUniversalLink

instance IsObjCObject (Id SFUniversalLink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFUniversalLink"

class IsNSObject a => IsSFUniversalLink a where
  toSFUniversalLink :: a -> Id SFUniversalLink

instance IsSFUniversalLink (Id SFUniversalLink) where
  toSFUniversalLink = unsafeCastId

instance IsNSObject (Id SFUniversalLink) where
  toNSObject = unsafeCastId

-- ---------- SFSafariExtensionViewController ----------

-- | Phantom type for @SFSafariExtensionViewController@.
data SFSafariExtensionViewController

instance IsObjCObject (Id SFSafariExtensionViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSafariExtensionViewController"

class IsNSViewController a => IsSFSafariExtensionViewController a where
  toSFSafariExtensionViewController :: a -> Id SFSafariExtensionViewController

instance IsSFSafariExtensionViewController (Id SFSafariExtensionViewController) where
  toSFSafariExtensionViewController = unsafeCastId

instance IsNSObject (Id SFSafariExtensionViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SFSafariExtensionViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id SFSafariExtensionViewController) where
  toNSViewController = unsafeCastId
