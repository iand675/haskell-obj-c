{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.FinderSync.Internal.Classes (
    module ObjC.FinderSync.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- FIFinderSync ----------

-- | A type to subclass to add badges, custom shortcut menus, and toolbar buttons to the Finder.
--
-- Subclass the FIFinderSync class when you want to customize the appearance of the Finder. Although the FIFinderSync class doesn’t provide any developer accessible API, it does adopt the ``FIFinderSyncProtocol`` protocol. This protocol declares methods you can implement to modify the appearance of the Finder. For more information on these methods, see ``FIFinderSyncProtocol``. To learn more about creating a Finder Sync extension, see [Finder Sync](https://developer.apple.com/library/archive/documentation/General/Conceptual/ExtensibilityPG/Finder.html#//apple_ref/doc/uid/TP40014214-CH15) in [App Extension Programming Guide](https://developer.apple.com/library/archive/documentation/General/Conceptual/ExtensibilityPG/index.html#//apple_ref/doc/uid/TP40014214).
--
-- ## See Also   - ``FinderSync/FIFinderSyncProtocol``
-- 
-- Phantom type for @FIFinderSync@.
data FIFinderSync

instance IsObjCObject (Id FIFinderSync) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FIFinderSync"

class IsNSObject a => IsFIFinderSync a where
  toFIFinderSync :: a -> Id FIFinderSync

instance IsFIFinderSync (Id FIFinderSync) where
  toFIFinderSync = unsafeCastId

instance IsNSObject (Id FIFinderSync) where
  toNSObject = unsafeCastId

-- ---------- FIFinderSyncController ----------

-- | A controller that acts as a bridge between your Finder Sync extension and the Finder itself.
--
-- Use the Finder Sync controller to configure your extension, to set badges on items in the Finder’s window, and to get a list of selected and targeted items.
-- 
-- Phantom type for @FIFinderSyncController@.
data FIFinderSyncController

instance IsObjCObject (Id FIFinderSyncController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "FIFinderSyncController"

class IsNSExtensionContext a => IsFIFinderSyncController a where
  toFIFinderSyncController :: a -> Id FIFinderSyncController

instance IsFIFinderSyncController (Id FIFinderSyncController) where
  toFIFinderSyncController = unsafeCastId

instance IsNSExtensionContext (Id FIFinderSyncController) where
  toNSExtensionContext = unsafeCastId

instance IsNSObject (Id FIFinderSyncController) where
  toNSObject = unsafeCastId
