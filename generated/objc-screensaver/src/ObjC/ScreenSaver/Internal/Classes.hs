{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.ScreenSaver.Internal.Classes (
    module ObjC.ScreenSaver.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- ScreenSaverDefaults ----------

-- | A class that defines a set of methods for saving and restoring user defaults for screen savers.
--
-- ``ScreenSaverDefaults`` gives you access to preference values you need to configure your screen saver. Because multiple apps can load a screen saver, you can’t use the standard <doc://com.apple.documentation/documentation/foundation/nsuserdefaults> object to store preferences. Instead, instantiate this class using the ``ScreenSaverDefaults/defaultsForModuleWithName:`` method, which takes your screen saver’s bundle identifier as a parameter. The resulting object gives you a way to store your preference values and associate them only with your screen saver. Use the inherited <doc://com.apple.documentation/documentation/foundation/nsuserdefaults> methods to load, store, or modify values.
-- 
-- Phantom type for @ScreenSaverDefaults@.
data ScreenSaverDefaults

instance IsObjCObject (Id ScreenSaverDefaults) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ScreenSaverDefaults"

class IsNSUserDefaults a => IsScreenSaverDefaults a where
  toScreenSaverDefaults :: a -> Id ScreenSaverDefaults

instance IsScreenSaverDefaults (Id ScreenSaverDefaults) where
  toScreenSaverDefaults = unsafeCastId

instance IsNSObject (Id ScreenSaverDefaults) where
  toNSObject = unsafeCastId

instance IsNSUserDefaults (Id ScreenSaverDefaults) where
  toNSUserDefaults = unsafeCastId

-- ---------- ScreenSaverView ----------

-- | An abstract class that defines the interface for subclassers to interact with the screen saver infrastructure.
--
-- ``ScreenSaverView`` provides the interface for your screen saver, including the content you animate onscreen and an optional configuration sheet. Create your own custom subclass and add it to your screen saver bundle. Use your subclass to create the animations that you want to appear onscreen, and to specify additional animation details.
--
-- - Note: When someone previews your screen saver in System Preferences, the system instantiates your ``ScreenSaverView`` subclass.
--
-- You can draw from your view’s ``ScreenSaverView/drawRect:`` method, or you can draw directly from the ``ScreenSaverView/animateOneFrame`` method. If you prefer to use the ``ScreenSaverView/drawRect:`` method, use the ``ScreenSaverView/animateOneFrame`` method to call the <doc://com.apple.documentation/documentation/appkit/nsview/1483475-setneedsdisplayinrect> method and specify the portions of your view that require updates.
-- 
-- Phantom type for @ScreenSaverView@.
data ScreenSaverView

instance IsObjCObject (Id ScreenSaverView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "ScreenSaverView"

class IsNSView a => IsScreenSaverView a where
  toScreenSaverView :: a -> Id ScreenSaverView

instance IsScreenSaverView (Id ScreenSaverView) where
  toScreenSaverView = unsafeCastId

instance IsNSObject (Id ScreenSaverView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id ScreenSaverView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id ScreenSaverView) where
  toNSView = unsafeCastId
